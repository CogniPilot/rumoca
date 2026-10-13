//! Entries of the checked FMI value-reference inventory.
//!
//! The attribute vocabulary below (`FmiStorageColumn`, `FmiStorageRun`,
//! `FmiCausality`, `FmiVariability`, `FmiInitial`) derives `Serialize` because
//! the private entry encoder writes those values through. They carry no
//! inventory identity, order, or membership. The gated thing is the *template
//! encoding*, the exact key set a version template consumes, and it has one
//! producer, [`super::metadata_projection::SerializedFmiVariables`], reachable only from the proved
//! [`super::FmiEventFreeCodegenView`] or [`super::FmiCCodegenView`].

use crate::{
    SolveDeclarationStart, SolveInitialValues, SolveVariableStorageRole, SolveVariableValueKind,
};
use rumoca_core::Span;
use serde::ser::SerializeMap;
use serde::{Deserialize, Serialize};

/// Numeric fields of the canonical checked FMI declaration projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FmiNumericAttribute {
    Start,
    StartFmi3,
    Minimum,
    Maximum,
    Nominal,
}

impl FmiNumericAttribute {
    pub const ALL: [Self; 5] = [
        Self::Start,
        Self::StartFmi3,
        Self::Minimum,
        Self::Maximum,
        Self::Nominal,
    ];

    pub const fn name(self) -> &'static str {
        match self {
            Self::Start => "start",
            Self::StartFmi3 => "start_fmi3",
            Self::Minimum => "minimum",
            Self::Maximum => "maximum",
            Self::Nominal => "nominal",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiStorageColumn {
    Y,
    P,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct FmiStorageRun {
    pub(super) column: FmiStorageColumn,
    pub(super) base: usize,
    pub(super) scalar_count: usize,
}

impl FmiStorageRun {
    #[must_use]
    pub const fn column(self) -> FmiStorageColumn {
        self.column
    }

    #[must_use]
    pub const fn base(self) -> usize {
        self.base
    }

    #[must_use]
    pub const fn scalar_count(self) -> usize {
        self.scalar_count
    }
}

/// What produces one inventory entry's value.
///
/// Every entry has exactly one of these, so a reader never has to ask a second
/// owner what stands behind a value reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FmiValueBacking {
    /// One checked Solve storage run executes the entry, in the role that run
    /// carries, naming one scalar identity per storage slot.
    SolveStorage {
        role: SolveVariableStorageRole,
        storage: FmiStorageRun,
    },
    /// No storage run backs the entry: the component derives its value from
    /// the complete current delay-time partition of the kernel it describes.
    MaxStepDuration,
}

impl FmiValueBacking {
    #[must_use]
    pub const fn storage(&self) -> Option<FmiStorageRun> {
        match self {
            Self::SolveStorage { storage, .. } => Some(*storage),
            Self::MaxStepDuration => None,
        }
    }

    #[must_use]
    pub const fn role(&self) -> Option<SolveVariableStorageRole> {
        match self {
            Self::SolveStorage { role, .. } => Some(*role),
            Self::MaxStepDuration => None,
        }
    }

    /// The one encoding of this owner: a typed discriminant, plus the storage
    /// facts a storage-backed entry's rendering reads.
    ///
    /// Written flat into the entry that carries it, so no reader has to infer
    /// the owner from the entry's name. Deliberately not a `Serialize`
    /// implementation: the owner is encodable only as part of the entry that
    /// carries it, which is itself encodable only through the proved
    /// type-state.
    pub(super) fn serialize_into<M: SerializeMap>(&self, entries: &mut M) -> Result<(), M::Error> {
        match self {
            Self::SolveStorage { role, storage, .. } => {
                entries.serialize_entry("backing", "solve_storage")?;
                entries.serialize_entry("role", role)?;
                entries.serialize_entry("storage", storage)
            }
            Self::MaxStepDuration => entries.serialize_entry("backing", "max_step_duration"),
        }
    }
}

/// One entry of the checked value-reference inventory.
///
/// Most entries project one Modelica declaration onto its storage run; the
/// maximum-step-duration local of SPEC_0044 §8 is an ordinary entry of the same
/// inventory whose [`FmiValueBacking`] names the component itself as the
/// evaluation owner.
#[derive(Debug, Clone)]
pub struct FmiVariable {
    pub(super) name: String,
    pub(super) value_kind: SolveVariableValueKind,
    pub(super) dimensions: Vec<u32>,
    pub(super) backing: FmiValueBacking,
    /// Absent where FMI forbids the attribute, which `initial="calculated"`
    /// does; a present value is the checked per-scalar start.
    pub(super) start_allowed: bool,
    pub(super) declaration_start: SolveDeclarationStart,
    pub(super) effective_input_default: Option<super::FmiInputInstantiationDefault>,
    pub(super) minimum: Option<SolveInitialValues>,
    pub(super) maximum: Option<SolveInitialValues>,
    pub(super) nominal: Option<SolveInitialValues>,
    /// The per-scalar literal start of a `String` entry, absent for every
    /// other kind and for a `String` whose declaration gives no literal.
    pub(super) text_start: Option<Vec<String>>,
    /// The declaration of the enumeration type of an `Enumeration` entry,
    /// absent for every other kind.
    pub(super) enumeration: Option<rumoca_core::EnumerationDeclaration>,
    pub(super) unit: Option<String>,
    pub(super) description: Option<String>,
    pub(super) causality: FmiCausality,
    /// The source `input`/`output` prefix, present only where it differs from
    /// the exported causality (a nested declaration exported `local`).
    pub(super) declared_causality: Option<FmiDeclaredCausality>,
    pub(super) variability: FmiVariability,
    pub(super) initial: Option<FmiInitial>,
    pub(super) tunable: bool,
    /// A parameter whose value is fixed at translation (MLS §4.5, §18.3:
    /// `final`, `Evaluate = true`, or a binding of such values alone).
    pub(super) evaluable: bool,
    pub(super) declaration: Option<Span>,
    pub(super) value_reference_fmi3: u32,
}

impl FmiVariable {
    pub fn numeric_attribute(&self, attribute: FmiNumericAttribute) -> Option<&SolveInitialValues> {
        match attribute {
            FmiNumericAttribute::Start | FmiNumericAttribute::StartFmi3 => self.start(),
            FmiNumericAttribute::Minimum => self.minimum(),
            FmiNumericAttribute::Maximum => self.maximum(),
            FmiNumericAttribute::Nominal => self.nominal(),
        }
    }
    /// A complete source Repeat can use FMI 3's scalar start broadcast.
    pub fn numeric_attribute_count(&self, attribute: FmiNumericAttribute) -> Option<usize> {
        if matches!(
            attribute,
            FmiNumericAttribute::Start | FmiNumericAttribute::StartFmi3
        ) {
            let values = self.typed_start()?;
            let broadcast = attribute == FmiNumericAttribute::StartFmi3 && values.is_repeat();
            return Some(if broadcast { 1 } else { values.len() });
        }
        self.numeric_attribute(attribute)
            .map(SolveInitialValues::len)
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Logical scalar extent of this checked declaration or derived local.
    #[must_use]
    pub fn scalar_count(&self) -> usize {
        self.backing
            .storage()
            .map_or(1, |storage| storage.scalar_count())
    }

    /// Derive one scalar name from this declaration's checked shape.
    #[must_use]
    pub fn scalar_name(&self, index: usize) -> Option<String> {
        if index >= self.scalar_count() {
            return None;
        }
        rumoca_core::scalar_component_name(&self.name, &self.dimensions, index)
    }

    /// Scalar identities are ephemeral projections, never retained owners.
    pub fn scalar_names(&self) -> impl ExactSizeIterator<Item = String> + '_ {
        (0..self.scalar_count()).map(|index| self.scalar_name(index).expect("checked scalar shape"))
    }

    /// The Solve storage role this entry carries, absent for the derived
    /// maximum-step-duration local that no run backs.
    #[must_use]
    pub const fn role(&self) -> Option<SolveVariableStorageRole> {
        self.backing.role()
    }

    #[must_use]
    pub const fn value_kind(&self) -> SolveVariableValueKind {
        self.value_kind
    }

    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }

    /// The sole backing/evaluation owner of this entry's value.
    #[must_use]
    pub const fn backing(&self) -> &FmiValueBacking {
        &self.backing
    }

    #[must_use]
    pub const fn storage(&self) -> Option<FmiStorageRun> {
        self.backing.storage()
    }

    /// The checked per-scalar start values, absent where FMI forbids the
    /// attribute.
    #[must_use]
    pub fn start(&self) -> Option<&SolveInitialValues> {
        self.typed_start().and_then(SolveDeclarationStart::real)
    }

    pub fn typed_start(&self) -> Option<&SolveDeclarationStart> {
        self.start_allowed.then(|| self.instantiation_start())
    }
    pub fn declaration_start(&self) -> &SolveDeclarationStart {
        &self.declaration_start
    }
    /// Original instantiation authority; it does not change source authorship.
    pub fn instantiation_start(&self) -> &SolveDeclarationStart {
        self.effective_input_default
            .as_ref()
            .map_or(&self.declaration_start, |default| default.values())
    }
    pub fn effective_input_default(&self) -> Option<&super::FmiInputInstantiationDefault> {
        self.effective_input_default.as_ref()
    }

    pub fn fmi3_type(&self) -> &'static str {
        match self.value_kind {
            SolveVariableValueKind::Real => "Float64",
            SolveVariableValueKind::Integer => "Int64",
            SolveVariableValueKind::Enumeration => "Enumeration",
            SolveVariableValueKind::Boolean => "Boolean",
            SolveVariableValueKind::String => "String",
        }
    }

    pub fn fmi2_integer_admitted(&self) -> bool {
        if !matches!(
            self.value_kind,
            SolveVariableValueKind::Integer | SolveVariableValueKind::Enumeration
        ) {
            return true;
        }
        let start_valid = match self.instantiation_start() {
            SolveDeclarationStart::Integer(values) | SolveDeclarationStart::Enumeration(values) => {
                values.runs().all(|run| match run {
                    crate::SolveExactInitialRun::Repeat { value, .. } => {
                        i32::try_from(*value).is_ok()
                    }
                    crate::SolveExactInitialRun::Literal { values, .. } => {
                        values.iter().all(|value| i32::try_from(*value).is_ok())
                    }
                })
            }
            SolveDeclarationStart::Missing { .. } => true,
            _ => false,
        };
        start_valid
            && [self.minimum(), self.maximum()]
                .into_iter()
                .flatten()
                .all(fmi2_integer_attribute_admitted)
    }

    #[must_use]
    pub fn minimum(&self) -> Option<&SolveInitialValues> {
        self.minimum.as_ref()
    }

    #[must_use]
    pub fn maximum(&self) -> Option<&SolveInitialValues> {
        self.maximum.as_ref()
    }

    #[must_use]
    pub fn nominal(&self) -> Option<&SolveInitialValues> {
        self.nominal.as_ref()
    }

    #[must_use]
    pub fn text_start(&self) -> Option<&[String]> {
        self.text_start.as_deref()
    }

    #[must_use]
    pub const fn enumeration(&self) -> Option<&rumoca_core::EnumerationDeclaration> {
        self.enumeration.as_ref()
    }

    #[must_use]
    pub fn unit(&self) -> Option<&str> {
        self.unit.as_deref()
    }

    #[must_use]
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    #[must_use]
    pub const fn causality(&self) -> FmiCausality {
        self.causality
    }

    /// The declared `input`/`output` prefix where the exported causality does
    /// not already state it, absent otherwise.
    #[must_use]
    pub const fn declared_causality(&self) -> Option<FmiDeclaredCausality> {
        self.declared_causality
    }

    #[must_use]
    pub const fn variability(&self) -> FmiVariability {
        self.variability
    }

    /// The `initial` attribute where Rumoca fixes it, absent where the
    /// standard's causality/variability default stands.
    #[must_use]
    pub const fn initial(&self) -> Option<FmiInitial> {
        self.initial
    }

    #[must_use]
    pub const fn is_tunable(&self) -> bool {
        self.tunable
    }

    /// The Modelica declaration this entry projects, absent for the derived
    /// maximum-step-duration local that no source declares.
    #[must_use]
    pub const fn declaration(&self) -> Option<Span> {
        self.declaration
    }

    #[must_use]
    pub const fn value_reference_fmi3(&self) -> u32 {
        self.value_reference_fmi3
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiCausality {
    Input,
    Output,
    Parameter,
    CalculatedParameter,
    Independent,
    Local,
}

/// A Modelica `input`/`output` prefix (MLS §4.4.2.2) that the exported
/// [`FmiCausality`] does not carry. Targets render it as a namespaced
/// per-variable annotation; it never changes the standard causality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiDeclaredCausality {
    Input,
    Output,
}

impl FmiDeclaredCausality {
    /// The exported causality that already states this prefix.
    #[must_use]
    pub const fn exported(self) -> FmiCausality {
        match self {
            Self::Input => FmiCausality::Input,
            Self::Output => FmiCausality::Output,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiVariability {
    Constant,
    Fixed,
    Tunable,
    Discrete,
    Continuous,
}

/// The FMI `initial` attribute, stated only where Rumoca fixes it rather than
/// leaving the standard's causality/variability default in force.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum FmiInitial {
    Exact,
    Approx,
    Calculated,
}

// FMI 2.0.5 §2.2.7 and FMI 3.0.2 §2.4.7: initialization belongs to the
// component metadata; version-specific XML writers do not infer it from roles.
pub(super) const fn initial_for_storage(
    role: SolveVariableStorageRole,
    causality: FmiCausality,
    variability: FmiVariability,
) -> Option<FmiInitial> {
    match causality {
        FmiCausality::Input | FmiCausality::Independent => None,
        FmiCausality::Parameter => Some(FmiInitial::Exact),
        FmiCausality::CalculatedParameter => Some(FmiInitial::Approx),
        FmiCausality::Output | FmiCausality::Local => match variability {
            FmiVariability::Constant => Some(FmiInitial::Exact),
            FmiVariability::Fixed | FmiVariability::Tunable => Some(FmiInitial::Approx),
            FmiVariability::Discrete | FmiVariability::Continuous => match role {
                SolveVariableStorageRole::State
                | SolveVariableStorageRole::DiscreteReal
                | SolveVariableStorageRole::DiscreteValue => Some(FmiInitial::Exact),
                _ => Some(FmiInitial::Calculated),
            },
        },
    }
}

impl FmiVariable {
    pub(super) fn is_initial_unknown(&self) -> bool {
        self.causality == FmiCausality::CalculatedParameter
            || (matches!(
                self.initial,
                Some(FmiInitial::Approx | FmiInitial::Calculated)
            ) && (self.causality == FmiCausality::Output
                || self.role() == Some(SolveVariableStorageRole::State)))
    }
}

/// Unchecked lowering input consumed only by `FmiComponent::construct`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FmiVariableInput {
    pub name: String,
    pub role: SolveVariableStorageRole,
    pub value_kind: SolveVariableValueKind,
    pub dimensions: Vec<u32>,
    pub start: SolveDeclarationStart,
    pub effective_input_default: Option<super::FmiInputDefaultInput>,
    pub minimum: Option<SolveInitialValues>,
    pub maximum: Option<SolveInitialValues>,
    pub nominal: Option<SolveInitialValues>,
    pub unit: Option<String>,
    pub description: Option<String>,
    pub causality: FmiCausality,
    /// The declared prefix where it differs from `causality`; construction
    /// refuses one the exported causality already states.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub declared_causality: Option<FmiDeclaredCausality>,
    pub variability: FmiVariability,
    pub tunable: bool,
    /// A parameter whose value is fixed at translation (MLS §4.5, §18.3).
    #[serde(default)]
    pub evaluable: bool,
    pub declaration: Span,
    /// The literal start of each scalar of a `String` declaration.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub text_start: Option<Vec<String>>,
    /// The declaration of the enumeration type of an `Enumeration`
    /// declaration (MLS §4.9.5).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enumeration: Option<rumoca_core::EnumerationDeclaration>,
}

impl FmiVariable {
    /// Original declaration ingredients only; no derived local or VR proof.
    pub(crate) fn source_input(&self) -> Option<FmiVariableInput> {
        Some(FmiVariableInput {
            name: self.name.clone(),
            role: self.role()?,
            value_kind: self.value_kind,
            dimensions: self.dimensions.clone(),
            start: self.declaration_start.clone(),
            effective_input_default: self
                .effective_input_default
                .as_ref()
                .map(|default| default.input()),
            minimum: self.minimum.clone(),
            maximum: self.maximum.clone(),
            nominal: self.nominal.clone(),
            unit: self.unit.clone(),
            description: self.description.clone(),
            causality: self.causality,
            declared_causality: self.declared_causality,
            variability: self.variability,
            tunable: self.tunable,
            evaluable: self.evaluable,
            declaration: self
                .declaration
                .expect("checked storage declaration retains its source"),
            text_start: self.text_start.clone(),
            enumeration: self.enumeration.clone(),
        })
    }
}

fn fmi2_integer_attribute_admitted(values: &SolveInitialValues) -> bool {
    values.runs().all(|run| {
        run.repeated_bits().map_or_else(
            || {
                run.literal_bits()
                    .is_some_and(|bits| bits.iter().copied().all(fmi2_integer_bits_admitted))
            },
            fmi2_integer_bits_admitted,
        )
    })
}

fn fmi2_integer_bits_admitted(bits: u64) -> bool {
    let value = f64::from_bits(bits);
    value >= f64::from(i32::MIN) && value <= f64::from(i32::MAX) && value.trunc() == value
}
