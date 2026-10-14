use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveVariableStorageRole {
    Parameter,
    Constant,
    ExternalInput,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveVariableValueKind {
    Real,
    Integer,
    Boolean,
    Enumeration,
    String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveVariableTimeDomain {
    Static,
    EventDiscrete,
    EventDiscontinuous,
    ContinuousTime,
}

impl SolveVariableTimeDomain {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Static => "static",
            Self::EventDiscrete => "event-discrete",
            Self::EventDiscontinuous => "event-discontinuous",
            Self::ContinuousTime => "continuous-time",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Deserialize, Serialize)]
pub struct SolveVariableStorageRun {
    pub base: ScalarSlot,
    pub scalar_count: usize,
    pub role: SolveVariableStorageRole,
    pub value_kind: SolveVariableValueKind,
}

/// Immutable typed declaration replayed independently of storage projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct SolveVariableDeclaration {
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
    time_domain: SolveVariableTimeDomain,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SolveVariableDeclarationWire {
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
    time_domain: SolveVariableTimeDomain,
}

impl<'de> Deserialize<'de> for SolveVariableDeclaration {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = SolveVariableDeclarationWire::deserialize(deserializer)?;
        let declaration = if wire.time_domain == SolveVariableTimeDomain::EventDiscontinuous {
            Self::event_discontinuous(wire.role, wire.value_kind)
                .map_err(serde::de::Error::custom)?
        } else {
            Self::new(wire.role, wire.value_kind)
        };
        if declaration.time_domain() != wire.time_domain {
            return Err(serde::de::Error::custom(format!(
                "{:?} {:?} storage requires time domain `{}`, not `{}`",
                wire.role,
                wire.value_kind,
                declaration.time_domain().as_str(),
                wire.time_domain.as_str()
            )));
        }
        Ok(declaration)
    }
}

impl SolveVariableDeclaration {
    pub const fn new(role: SolveVariableStorageRole, value_kind: SolveVariableValueKind) -> Self {
        Self {
            role,
            value_kind,
            time_domain: default_time_domain(role),
        }
    }

    pub fn event_discontinuous(
        role: SolveVariableStorageRole,
        value_kind: SolveVariableValueKind,
    ) -> Result<Self, SolveVariableDeclarationError> {
        if !matches!(
            role,
            SolveVariableStorageRole::Algebraic | SolveVariableStorageRole::Output
        ) || value_kind != SolveVariableValueKind::Real
        {
            return Err(SolveVariableDeclarationError { role, value_kind });
        }
        Ok(Self {
            role,
            value_kind,
            time_domain: SolveVariableTimeDomain::EventDiscontinuous,
        })
    }

    pub const fn role(self) -> SolveVariableStorageRole {
        self.role
    }

    pub const fn value_kind(self) -> SolveVariableValueKind {
        self.value_kind
    }

    pub const fn time_domain(self) -> SolveVariableTimeDomain {
        self.time_domain
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SolveVariableDeclarationError {
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
}

impl std::fmt::Display for SolveVariableDeclarationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "{:?} {:?} storage cannot be event-discontinuous",
            self.role, self.value_kind
        )
    }
}

impl std::error::Error for SolveVariableDeclarationError {}

const fn default_time_domain(role: SolveVariableStorageRole) -> SolveVariableTimeDomain {
    match role {
        SolveVariableStorageRole::Parameter | SolveVariableStorageRole::Constant => {
            SolveVariableTimeDomain::Static
        }
        SolveVariableStorageRole::DiscreteReal | SolveVariableStorageRole::DiscreteValue => {
            SolveVariableTimeDomain::EventDiscrete
        }
        SolveVariableStorageRole::ExternalInput
        | SolveVariableStorageRole::State
        | SolveVariableStorageRole::Algebraic
        | SolveVariableStorageRole::Output => SolveVariableTimeDomain::ContinuousTime,
    }
}

impl SolveVariableStorageRun {
    pub fn event_iteration_kind(self) -> Option<EventIterationValueKind> {
        match (self.role, self.value_kind) {
            (SolveVariableStorageRole::DiscreteReal, SolveVariableValueKind::Real) => {
                Some(EventIterationValueKind::Real)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Integer) => {
                Some(EventIterationValueKind::Integer)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Boolean) => {
                Some(EventIterationValueKind::Boolean)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Enumeration) => {
                Some(EventIterationValueKind::Enumeration)
            }
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct SolveLayout {
    /// Dense DAE variable ordinal to its first Solve storage slot.
    ///
    /// Scalar `k` of a variable is stored at `base + k` in the same column.
    /// This is the canonical cross-phase coordinate map; display names are not
    /// used to recover compiler identity.
    pub variable_storage_runs: Vec<SolveVariableStorageRun>,
    /// Canonical typed DAE declarations in the same dense identity order.
    pub variable_declarations: Vec<SolveVariableDeclaration>,
    pub state_scalar_count: usize,
    pub algebraic_scalar_count: usize,
    pub output_scalar_count: usize,
    pub parameter_count: usize,
    pub compiled_parameter_len: usize,
    /// One name and shape per input declaration; scalar names are derived.
    pub input_scalar_names: SolveScalarNames,
    pub discrete_real_scalar_names: Vec<String>,
    pub discrete_valued_scalar_names: Vec<String>,
    pub relation_memory_parameter_indices: Vec<usize>,
    pub initial_event_parameter_index: Option<usize>,
    /// P-slot that is true only while applying the final simulation event.
    pub terminal_event_parameter_index: Option<usize>,
    /// Hidden P-slot used by initialization residuals that contain
    /// `homotopy(actual, simplified)`.
    ///
    /// The initialization driver advances this value from zero to one. Models
    /// without homotopy expressions omit the slot entirely.
    pub initial_homotopy_parameter_index: Option<usize>,
    /// Snapshot bindings for `__pre__.*` parameters created by DAE-IR
    /// pre_lowering. At event entry the runtime copies each source slot into
    /// the corresponding dest P-slot before the event equations evaluate.
    pub pre_param_bindings: Vec<PreParamBinding>,
}

impl SolveLayout {
    pub fn variable_scalar_slot(&self, variable: usize, scalar: usize) -> Option<ScalarSlot> {
        let run = self.variable_storage_runs.get(variable)?;
        if scalar >= run.scalar_count {
            return None;
        }
        match run.base {
            ScalarSlot::Y { index, .. } => index.checked_add(scalar).map(scalar_slot_y),
            ScalarSlot::P { index, .. } => index.checked_add(scalar).map(scalar_slot_p),
            ScalarSlot::Time | ScalarSlot::Constant(_) => None,
        }
    }

    pub fn state_scalar_count(&self) -> usize {
        self.state_scalar_count
    }

    pub fn algebraic_scalar_count(&self) -> usize {
        self.algebraic_scalar_count
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.state_scalar_count
            .saturating_add(self.algebraic_scalar_count)
            .saturating_add(self.output_scalar_count)
    }

    pub fn input_scalar_names(&self) -> &SolveScalarNames {
        &self.input_scalar_names
    }

    pub fn input_parameter_index(&self, name: &str) -> Option<usize> {
        self.input_scalar_names
            .position(name)
            .map(|offset| self.parameter_count + offset)
    }

    pub fn discrete_real_parameter_index(&self, name: &str) -> Option<usize> {
        self.discrete_real_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| self.parameter_count + self.input_scalar_names.len() + offset)
    }

    pub fn discrete_valued_parameter_index(&self, name: &str) -> Option<usize> {
        self.discrete_valued_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| {
                self.parameter_count
                    + self.input_scalar_names.len()
                    + self.discrete_real_scalar_names.len()
                    + offset
            })
    }
}
