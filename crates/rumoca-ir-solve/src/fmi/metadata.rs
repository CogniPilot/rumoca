//! Entries of the checked FMI value-reference inventory.
//!
//! The attribute vocabulary below (`FmiStorageColumn`, `FmiStorageRun`,
//! `FmiCausality`, `FmiVariability`, `FmiInitial`) derives `Serialize` because
//! the private entry encoder writes those values through. They carry no
//! inventory identity, order, or membership. The gated thing is the *template
//! encoding*, the exact key set a version template consumes, and it has one
//! producer, [`SerializedFmiVariables`], reachable only from the proved
//! [`super::FmiEventFreeCodegenView`] or [`super::FmiCCodegenView`].

use crate::{SolveVariableStorageRole, SolveVariableValueKind};
use rumoca_core::Span;
use serde::ser::{SerializeMap, Serializer};
use serde::{Deserialize, Serialize};

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
        scalar_names: Vec<String>,
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
    fn serialize_into<M: SerializeMap>(&self, entries: &mut M) -> Result<(), M::Error> {
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
    pub(super) start: Option<Vec<f64>>,
    pub(super) minimum: Option<Vec<f64>>,
    pub(super) maximum: Option<Vec<f64>>,
    pub(super) nominal: Option<Vec<f64>>,
    pub(super) unit: Option<String>,
    pub(super) description: Option<String>,
    pub(super) causality: FmiCausality,
    pub(super) variability: FmiVariability,
    pub(super) initial: Option<FmiInitial>,
    pub(super) tunable: bool,
    pub(super) declaration: Option<Span>,
    pub(super) value_reference_fmi3: u32,
}

impl FmiVariable {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// This entry's scalar identities, in scalar order.
    ///
    /// A storage-backed entry names one scalar per storage slot, and its
    /// backing owns those names. The derived local is one scalar whose
    /// identity is the entry's own name, lent here rather than stored a second
    /// time, so an FMI 2 scalar projection covers it without special-casing
    /// the name.
    #[must_use]
    pub fn scalar_names(&self) -> &[String] {
        match &self.backing {
            FmiValueBacking::SolveStorage { scalar_names, .. } => scalar_names,
            FmiValueBacking::MaxStepDuration => std::slice::from_ref(&self.name),
        }
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

    #[must_use]
    pub const fn causality(&self) -> FmiCausality {
        self.causality
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

/// The one inventory encoding, borrowed from the single owned inventory.
///
/// [`FmiVariable`] deliberately implements no `Serialize` of its own, so
/// possessing an entry (or a whole unrestricted inventory through
/// [`super::FmiComponent::variables`] or [`super::FmiCodegenView::metadata`])
/// is not the ability to encode one. This wrapper is the only encoder, it is
/// private to the FMI module, and it borrows: no entry is cloned, no order is
/// re-derived, and no second inventory exists to drift.
///
/// The only construction site is [`super::FmiEventFreeCodegenView`]'s
/// `Serialize`, which is reachable only after the narrowing proved the
/// component event-free.
pub(super) struct SerializedFmiVariables<'inventory>(&'inventory [FmiVariable]);

impl<'inventory> SerializedFmiVariables<'inventory> {
    pub(super) const fn borrowing(variables: &'inventory [FmiVariable]) -> Self {
        Self(variables)
    }
}

impl Serialize for SerializedFmiVariables<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_seq(self.0.iter().map(SerializedFmiVariable))
    }
}

/// One entry of the encoding above, in the inventory's own order.
///
/// Rendering reads the same entry the checked views expose: the derived scalar
/// identities rather than the stored ones, the typed backing owner, and a
/// `start` key only where the entry has one to give.
struct SerializedFmiVariable<'entry>(&'entry FmiVariable);

impl Serialize for SerializedFmiVariable<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let variable = self.0;
        let mut entry = serializer.serialize_map(None)?;
        entry.serialize_entry("name", &variable.name)?;
        entry.serialize_entry("scalar_names", variable.scalar_names())?;
        entry.serialize_entry("value_kind", &variable.value_kind)?;
        entry.serialize_entry("dimensions", &variable.dimensions)?;
        variable.backing.serialize_into(&mut entry)?;
        if let Some(start) = &variable.start {
            entry.serialize_entry("start", start)?;
        }
        entry.serialize_entry("minimum", &variable.minimum)?;
        entry.serialize_entry("maximum", &variable.maximum)?;
        entry.serialize_entry("nominal", &variable.nominal)?;
        entry.serialize_entry("unit", &variable.unit)?;
        entry.serialize_entry("description", &variable.description)?;
        entry.serialize_entry("causality", &variable.causality)?;
        entry.serialize_entry("variability", &variable.variability)?;
        entry.serialize_entry("initial", &variable.initial)?;
        entry.serialize_entry("initial_unknown", &variable.is_initial_unknown())?;
        entry.serialize_entry("tunable", &variable.tunable)?;
        entry.serialize_entry("declaration", &variable.declaration)?;
        entry.serialize_entry("value_reference_fmi3", &variable.value_reference_fmi3)?;
        entry.end()
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
    fn is_initial_unknown(&self) -> bool {
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
pub struct FmiVariableInput {
    pub name: String,
    pub scalar_names: Vec<String>,
    pub role: SolveVariableStorageRole,
    pub value_kind: SolveVariableValueKind,
    pub dimensions: Vec<u32>,
    pub start: Vec<f64>,
    pub minimum: Option<Vec<f64>>,
    pub maximum: Option<Vec<f64>>,
    pub nominal: Option<Vec<f64>>,
    pub unit: Option<String>,
    pub description: Option<String>,
    pub causality: FmiCausality,
    pub variability: FmiVariability,
    pub tunable: bool,
    pub declaration: Span,
}
