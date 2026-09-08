//! Entries of the checked FMI value-reference inventory.
//!
//! The attribute vocabulary below (`SolveStorageColumn`, `FmiStorageRun`,
//! `FmiCausality`, `FmiVariability`, `FmiInitial`) derives `Serialize` because
//! the private entry encoder writes those values through. They carry no
//! inventory identity, order, or membership. The gated thing is the *template
//! encoding*, the exact key set a version template consumes, and it has one
//! producer, [`SerializedFmiVariables`], reachable only from the proved
//! [`super::FmiEventFreeCodegenView`].

use crate::{
    SolveStorageColumn, SolveVariableId, SolveVariableStorageRole, SolveVariableValueKind,
};
use rumoca_core::Span;
use serde::ser::{SerializeMap, Serializer};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct FmiStorageRun {
    pub(super) column: SolveStorageColumn,
    pub(super) base: usize,
    pub(super) scalar_count: usize,
}

impl FmiStorageRun {
    #[must_use]
    pub const fn column(self) -> SolveStorageColumn {
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
    pub(super) source_id: Option<SolveVariableId>,
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
    pub(super) write_policy: FmiWritePolicy,
    pub(super) tunable: bool,
    pub(super) declaration: Option<Span>,
    pub(super) value_reference_fmi3: u32,
}

impl FmiVariable {
    #[must_use]
    pub const fn source_id(&self) -> Option<SolveVariableId> {
        self.source_id
    }

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
    pub const fn write_policy(&self) -> FmiWritePolicy {
        self.write_policy
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
        entry.serialize_entry("tunable", &variable.tunable)?;
        entry.serialize_entry("declaration", &variable.declaration)?;
        entry.serialize_entry("value_reference_fmi3", &variable.value_reference_fmi3)?;
        entry.end()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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

/// The FMI `initial` fact a continuous state carries, in the form the write
/// tables consume.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiStateInitial {
    /// The state start is a fixed exact value.
    Exact,
    /// The state start is an approximation an initialization may overwrite.
    Approx,
}

/// Whether a continuous state carries construction-issued `reinit = false`
/// evidence, in the form the write tables consume.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiStateReinit {
    /// No source event action reinitializes the state.
    False,
    /// Source event semantics may reinitialize the state.
    Reinitializable,
}

/// The FMI write facts of one projected variable, in the form both version
/// tables match on directly.
///
/// Each variant names the fact that decides a variable's writability, not a
/// mode set: the same policy yields different modes under FMI 2 and FMI 3, so
/// the mode sets live in the version tables ([`super::Fmi2WriteModes`],
/// [`super::Fmi3WriteModes`]) and never here. A variable whose combination the
/// catalog never issues has no policy, so construction fails closed rather than
/// guessing one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FmiWritePolicy {
    ReadOnly,
    /// A parameter with `variability = fixed`.
    FixedParameter,
    /// A parameter with `variability = tunable`.
    TunableParameter,
    /// An input with `variability = continuous`.
    ContinuousInput,
    /// An input with `variability = discrete`.
    DiscreteInput,
    /// A continuous state, carrying the two facts the version tables read.
    ContinuousState {
        initial: FmiStateInitial,
        reinit: FmiStateReinit,
    },
}

#[cfg(test)]
mod write_mode_oracle {
    use super::{FmiCausality, FmiStateInitial, FmiStateReinit, FmiVariability, FmiWritePolicy};
    use crate::fmi::fmi_write_policy;
    use crate::fmi::{Fmi2WriteMode, Fmi2WriteModes, Fmi3WriteMode, Fmi3WriteModes};

    /// One catalog-issuable fact tuple, plus a label for the divergence
    /// witness. The mask is decided through the production `fmi_write_policy`,
    /// never assumed, so this oracle exercises the shipping decision path.
    #[derive(Clone, Copy)]
    struct Fact {
        label: &'static str,
        causality: FmiCausality,
        variability: FmiVariability,
        state: Option<(FmiStateInitial, FmiStateReinit)>,
    }

    /// The five lifecycle modes, each paired with its FMI 2 and FMI 3 bit and a
    /// short label. The oracle sweeps every (fact, mode) cell.
    #[derive(Clone, Copy)]
    struct Mode {
        label: &'static str,
        fmi2: Fmi2WriteMode,
        fmi3: Fmi3WriteMode,
    }

    const MODES: [Mode; 5] = [
        Mode {
            label: "instantiated",
            fmi2: Fmi2WriteMode::Instantiated,
            fmi3: Fmi3WriteMode::Instantiated,
        },
        Mode {
            label: "initialization",
            fmi2: Fmi2WriteMode::InitializationMode,
            fmi3: Fmi3WriteMode::InitializationMode,
        },
        Mode {
            label: "event",
            fmi2: Fmi2WriteMode::EventMode,
            fmi3: Fmi3WriteMode::EventMode,
        },
        Mode {
            label: "continuous",
            fmi2: Fmi2WriteMode::ContinuousTimeMode,
            fmi3: Fmi3WriteMode::ContinuousTimeMode,
        },
        Mode {
            label: "step",
            fmi2: Fmi2WriteMode::StepComplete,
            fmi3: Fmi3WriteMode::StepMode,
        },
    ];

    /// Every fact tuple the Modelica-to-ME catalog issues, one per policy shape
    /// and one per continuous-state initial/reinit corner. A read-only output
    /// stands for every calculated variable. If a real catalog can issue a
    /// tuple absent here, its cells go unchecked; the production
    /// `checked_variable` builds its policy from these same facts, so a tuple it
    /// admits but this list omits is the gap to guard against, not a false pass.
    fn catalog_facts() -> Vec<Fact> {
        use FmiStateInitial::{Approx, Exact};
        use FmiStateReinit::{False, Reinitializable};
        vec![
            Fact {
                label: "read_only_output",
                causality: FmiCausality::Output,
                variability: FmiVariability::Continuous,
                state: None,
            },
            Fact {
                label: "fixed_parameter",
                causality: FmiCausality::Parameter,
                variability: FmiVariability::Fixed,
                state: None,
            },
            Fact {
                label: "tunable_parameter",
                causality: FmiCausality::Parameter,
                variability: FmiVariability::Tunable,
                state: None,
            },
            Fact {
                label: "continuous_input",
                causality: FmiCausality::Input,
                variability: FmiVariability::Continuous,
                state: None,
            },
            Fact {
                label: "discrete_input",
                causality: FmiCausality::Input,
                variability: FmiVariability::Discrete,
                state: None,
            },
            Fact {
                label: "state_exact_reinit_false",
                causality: FmiCausality::Local,
                variability: FmiVariability::Continuous,
                state: Some((Exact, False)),
            },
            Fact {
                label: "state_exact_reinitializable",
                causality: FmiCausality::Local,
                variability: FmiVariability::Continuous,
                state: Some((Exact, Reinitializable)),
            },
            Fact {
                label: "state_approx_reinit_false",
                causality: FmiCausality::Local,
                variability: FmiVariability::Continuous,
                state: Some((Approx, False)),
            },
            Fact {
                label: "state_approx_reinitializable",
                causality: FmiCausality::Local,
                variability: FmiVariability::Continuous,
                state: Some((Approx, Reinitializable)),
            },
        ]
    }

    // Independent fact classifiers. These read the raw tuple, never a policy or
    // a mask, so the predicates below cannot agree with the implementation by
    // sharing its code.
    fn is_input(fact: Fact) -> bool {
        matches!(fact.causality, FmiCausality::Input)
    }
    fn is_continuous_input(fact: Fact) -> bool {
        is_input(fact) && matches!(fact.variability, FmiVariability::Continuous)
    }
    fn is_fixed_parameter(fact: Fact) -> bool {
        matches!(fact.causality, FmiCausality::Parameter)
            && matches!(fact.variability, FmiVariability::Fixed)
    }
    fn is_tunable_parameter(fact: Fact) -> bool {
        matches!(fact.causality, FmiCausality::Parameter)
            && matches!(fact.variability, FmiVariability::Tunable)
    }
    fn is_state(fact: Fact) -> bool {
        fact.state.is_some()
    }
    fn state_is_exact(fact: Fact) -> bool {
        matches!(fact.state, Some((FmiStateInitial::Exact, _)))
    }
    fn state_is_reinit_false(fact: Fact) -> bool {
        matches!(fact.state, Some((_, FmiStateReinit::False)))
    }

    // FMI 2.0.5 admissible-write predicates, one per mode, transcribed from the
    // clauses cited in `write_modes::Fmi2WriteModes::of`.
    fn fmi2_admits(mode: &str, fact: Fact) -> bool {
        match mode {
            // §2.1.3: variability != constant, initial in {exact, approx},
            // inputs excluded. Parameters and states qualify; inputs do not.
            "instantiated" => {
                is_fixed_parameter(fact) || is_tunable_parameter(fact) || is_state(fact)
            }
            // fmi2SetReal: initial = exact, or causality = input.
            "initialization" => {
                is_fixed_parameter(fact)
                    || is_tunable_parameter(fact)
                    || is_input(fact)
                    || (is_state(fact) && state_is_exact(fact))
            }
            // Footnote 4: causality = input, or a tunable parameter. No state.
            "event" => is_input(fact) || is_tunable_parameter(fact),
            // Footnote 5: every continuous state, plus continuous inputs.
            "continuous" => is_state(fact) || is_continuous_input(fact),
            // Co-Simulation exposes inputs and tunable parameters, not states.
            "step" => is_input(fact) || is_tunable_parameter(fact),
            other => panic!("unknown mode {other}"),
        }
    }

    // FMI 3.0.2 admissible-write predicates, one per mode, transcribed from the
    // clauses cited in `write_modes::Fmi3WriteModes::of`.
    fn fmi3_admits(mode: &str, fact: Fact) -> bool {
        match mode {
            // variability != constant, initial in {exact, approx}; inputs
            // included (their initial defaults to exact).
            "instantiated" => {
                is_fixed_parameter(fact)
                    || is_tunable_parameter(fact)
                    || is_input(fact)
                    || is_state(fact)
            }
            // initial = exact; inputs qualify through that default.
            "initialization" => {
                is_fixed_parameter(fact)
                    || is_tunable_parameter(fact)
                    || is_input(fact)
                    || (is_state(fact) && state_is_exact(fact))
            }
            // Inputs and tunable parameters, plus states with reinit = false.
            "event" => {
                is_input(fact)
                    || is_tunable_parameter(fact)
                    || (is_state(fact) && state_is_reinit_false(fact))
            }
            // Continuous inputs and continuous-time states.
            "continuous" => is_continuous_input(fact) || is_state(fact),
            // Co-Simulation exposes inputs and tunable parameters, not states.
            "step" => is_input(fact) || is_tunable_parameter(fact),
            other => panic!("unknown mode {other}"),
        }
    }

    fn policy_of(fact: Fact) -> FmiWritePolicy {
        fmi_write_policy(fact.causality, fact.variability, fact.state)
            .unwrap_or_else(|| panic!("catalog fact {} must map to a write policy", fact.label))
    }

    /// Every (fact, mode) cell must satisfy the standard predicate as decided by
    /// the production `fmi_write_policy` plus the version mask. Vacuity: the
    /// predicates are transcribed from the clauses, not read back from the mask,
    /// so a mask that admitted or denied everything would fail here; and the
    /// per-mode coverage guard below refuses to let any mode column be all-true
    /// or all-false, so a predicate error that flattened a column is caught too.
    #[test]
    fn every_cell_matches_the_standard_predicate() {
        for fact in catalog_facts() {
            let policy = policy_of(fact);
            let fmi2 = Fmi2WriteModes::of(policy);
            let fmi3 = Fmi3WriteModes::of(policy);
            for mode in MODES {
                assert_eq!(
                    fmi2.admits(mode.fmi2),
                    fmi2_admits(mode.label, fact),
                    "FMI2 {} / {} disagrees with the standard predicate",
                    fact.label,
                    mode.label,
                );
                assert_eq!(
                    fmi3.admits(mode.fmi3),
                    fmi3_admits(mode.label, fact),
                    "FMI3 {} / {} disagrees with the standard predicate",
                    fact.label,
                    mode.label,
                );
            }
        }
    }

    /// No mode column may be uniformly admitted or uniformly denied across the
    /// catalog. Vacuity: without this, an all-allow or all-deny mask that also
    /// had an all-allow or all-deny predicate would pass the equality test
    /// above; this refuses that degenerate shape for both versions.
    #[test]
    fn every_mode_has_an_admitted_and_a_denied_cell() {
        let facts = catalog_facts();
        for mode in MODES {
            for (version, admits) in [
                ("FMI2", fmi2_admits as fn(&str, Fact) -> bool),
                ("FMI3", fmi3_admits as fn(&str, Fact) -> bool),
            ] {
                let admitted = facts
                    .iter()
                    .filter(|fact| admits(mode.label, **fact))
                    .count();
                assert!(
                    admitted > 0 && admitted < facts.len(),
                    "{version} {} must admit some cells and deny some cells",
                    mode.label,
                );
            }
        }
    }

    /// The exact set of cells where FMI 2 and FMI 3 disagree. Vacuity: an empty
    /// witness would mean the two tables collapsed into one, the defect this
    /// change exists to prevent, so the expected set is nonempty and named. The
    /// masks come from the production `of`, so a shared-table regression that
    /// removed the version difference would fail here.
    #[test]
    fn the_two_versions_differ_only_at_named_cells() {
        let mut divergent = catalog_facts()
            .into_iter()
            .flat_map(|fact| {
                let policy = policy_of(fact);
                let fmi2 = Fmi2WriteModes::of(policy);
                let fmi3 = Fmi3WriteModes::of(policy);
                MODES.into_iter().filter_map(move |mode| {
                    (fmi2.admits(mode.fmi2) != fmi3.admits(mode.fmi3))
                        .then_some((fact.label, mode.label))
                })
            })
            .collect::<Vec<_>>();
        divergent.sort_unstable();
        assert_eq!(
            divergent,
            vec![
                // FMI 2 excludes inputs from Instantiated; FMI 3 includes them.
                ("continuous_input", "instantiated"),
                ("discrete_input", "instantiated"),
                // FMI 3 Event Mode admits a reinit = false state; FMI 2 admits
                // no state in Event Mode.
                ("state_approx_reinit_false", "event"),
                ("state_exact_reinit_false", "event"),
            ],
        );
    }
}
