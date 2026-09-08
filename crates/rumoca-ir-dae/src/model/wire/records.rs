//! Private current-schema wire records.
//!
//! These mirror the serialized column names while staying distinct from
//! every invariant-bearing arena entry: deserialization can produce only
//! wire data, and the records enter the IR solely through the checked
//! operations `reconstruct` replays. Facts an operation itself produces
//! are absent from every record here.

use super::*;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ValueTypeWire {
    pub(super) scalar: ScalarType,
    pub(super) dimensions: Box<[u32]>,
    pub(super) record_name: Option<rumoca_core::VarName>,
    pub(super) record_fields: Box<[RecordFieldTypeWire]>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct RecordFieldTypeWire {
    pub(super) name: rumoca_core::VarName,
    pub(super) value_type: u32,
}

impl ValueTypeWire {
    pub(super) fn as_primitive_value_type(&self) -> Result<ValueType, DaeConstructionError> {
        if self.scalar == ScalarType::Record
            || self.record_name.is_some()
            || !self.record_fields.is_empty()
        {
            return Err(DaeConstructionError::MalformedWire {
                column: "value_types",
            });
        }
        Ok(ValueType::array(self.scalar, self.dimensions.clone()))
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct VariableEntryWire {
    pub(super) name: rumoca_core::VarName,
    pub(super) source_occurrence: rumoca_core::SourceOccurrenceId,
    pub(super) role: VariableRole,
    pub(super) variability: ExpressionVariability,
    pub(super) value_type: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) declaration: DaeProvenance,
    pub(super) attributes: Option<VariableAttributesInput>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct VariableAttributesInput {
    pub(super) component_ref: Option<rumoca_core::ComponentReference>,
    pub(super) binding: Option<u32>,
    pub(super) start: Option<u32>,
    pub(super) fixed: rumoca_core::Fixity,
    pub(super) min: Option<u32>,
    pub(super) max: Option<u32>,
    pub(super) nominal: Option<u32>,
    pub(super) unit: Option<String>,
    pub(super) state_select: rumoca_core::StateSelect,
    pub(super) description: Option<String>,
    pub(super) causality: VariableCausality,
    pub(super) is_tunable: bool,
    pub(super) is_held: bool,
    pub(super) origin: VariableOrigin,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct FunctionEntryWire<Name = rumoca_core::VarName> {
    pub(super) name: Name,
    pub(super) parameters: Vec<FunctionNamedValueWire<Name>>,
    pub(super) outputs: Vec<FunctionNamedValueWire<Name>>,
    pub(super) locals: Vec<FunctionNamedValueWire<Name>>,
    pub(super) body: FunctionBodyInput<Name>,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) declaration: DaeProvenance,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum FunctionBodyInput<Name = rumoca_core::VarName> {
    Modelica {
        statements: Vec<FunctionStatementInput>,
    },
    External {
        body: ExternalBodyInput<Name>,
    },
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ExternalBodyInput<Name = rumoca_core::VarName> {
    pub(super) purity: FunctionPurity,
    pub(super) language: ExternalLanguage,
    pub(super) symbol: Name,
    pub(super) arguments: Vec<ExternalArgumentEntry>,
    pub(super) result: Option<u32>,
    pub(super) linkage: ExternalLinkage,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct FunctionNamedValueWire<Name = rumoca_core::VarName> {
    pub(super) name: Name,
    pub(super) value_type: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) declaration: DaeProvenance,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum FunctionStatementInput {
    Assignment {
        target: u32,
        rhs: u32,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
    AssignmentGroup {
        assignments: Vec<FunctionAssignmentInput>,
        conditional: Option<FunctionConditionalInput>,
    },
    Assertion {
        condition: u32,
        message: u32,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
    For {
        domain: u32,
        targets: Vec<u32>,
        iteration_locals: Vec<u32>,
        statements: Vec<FunctionStatementInput>,
        #[serde(deserialize_with = "deserialize_provenance")]
        begin_provenance: DaeProvenance,
        #[serde(deserialize_with = "deserialize_provenance")]
        finish_provenance: DaeProvenance,
    },
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct FunctionConditionalInput {
    pub(super) conditions: Vec<u32>,
    pub(super) branches: Vec<Vec<u32>>,
    pub(super) fallback: Vec<u32>,
}

#[derive(Clone, Copy, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct FunctionAssignmentInput {
    pub(super) target: u32,
    pub(super) rhs: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct DomainEntryWire {
    pub(super) parent: Option<u32>,
    pub(super) domain: rumoca_core::StructuredIndexDomain,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct DiscreteRealEquationWire {
    pub(super) residual: u32,
    pub(super) activation: DiscreteRealActivationWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Clone, Copy, Deserialize)]
#[serde(rename_all = "snake_case", tag = "kind", content = "variable")]
pub(super) enum ModelEventTargetWire {
    DiscreteReal(u32),
    DiscreteValue(u32),
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ModelEventDefinitionWire {
    pub(super) target: ModelEventTargetWire,
    pub(super) value: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ModelEventStepWire {
    pub(super) trigger: u32,
    pub(super) guard: u32,
    pub(super) clock: Option<u32>,
    pub(super) definitions: Vec<ModelEventDefinitionWire>,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ModelEventTransactionWire {
    pub(super) targets: Vec<ModelEventTargetWire>,
    pub(super) steps: Vec<ModelEventStepWire>,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct InitialDiscreteValueWire {
    pub(super) target: u32,
    pub(super) value: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum DiscreteRealActivationWire {
    Always,
    When { trigger: u32, guard: u32 },
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum EquationOperationInput {
    Residual {
        residual: u32,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
    Structured {
        domain: u32,
        scalar_view: rumoca_core::ComprehensionScalarView,
        bodies: Vec<u32>,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct DiscreteValueOwnerWire<Targets = Vec<u32>> {
    pub(super) targets: Targets,
    pub(super) branches: Vec<DiscreteValueBranchWire>,
    pub(super) structure: Option<StructuredDiscreteValueWire>,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Serialize, Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
pub(super) struct StructuredDiscreteValueWire {
    pub(super) domain: u32,
    pub(super) scalar_view: rumoca_core::ComprehensionScalarView,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct DiscreteValueBranchWire {
    pub(super) activation: DiscreteBranchActivationWire,
    pub(super) values: Vec<DiscreteValueActionWire>,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Serialize, Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum DiscreteBranchActivationWire {
    Always,
    When { trigger: u32, guard: u32 },
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct DiscreteValueActionWire {
    pub(super) value: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

pub(super) fn discrete_value_owner_output(
    storage: &FrozenStorage,
) -> Vec<DiscreteValueOwnerWire<&[u32]>> {
    storage
        .discrete_value_owners
        .iter()
        .map(|owner| {
            let targets = &storage.discrete_value_targets[owner.targets.indices()];
            let branches = storage.discrete_value_branches[owner.branches.indices()]
                .iter()
                .map(|branch| {
                    let values = branch
                        .values
                        .indices()
                        .map(|index| DiscreteValueActionWire {
                            value: storage.discrete_value_branch_values[index],
                            provenance: storage.discrete_value_branch_value_provenance[index],
                        })
                        .collect();
                    let activation = match branch.activation {
                        DiscreteBranchActivationEntry::Always => {
                            DiscreteBranchActivationWire::Always
                        }
                        DiscreteBranchActivationEntry::When { trigger, guard } => {
                            DiscreteBranchActivationWire::When { trigger, guard }
                        }
                    };
                    DiscreteValueBranchWire {
                        activation,
                        values,
                        provenance: branch.provenance,
                    }
                })
                .collect();
            DiscreteValueOwnerWire {
                targets,
                branches,
                structure: owner
                    .structure
                    .map(|structure| StructuredDiscreteValueWire {
                        domain: structure.domain,
                        scalar_view: structure.scalar_view,
                    }),
                provenance: owner.provenance,
            }
        })
        .collect()
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum ConditionNodeWire {
    Initial,
    Always,
    Relation(u32),
    Discrete(u32),
    Clock(u32),
    Not(u32),
    And {
        lhs: u32,
        rhs: u32,
    },
    Or {
        lhs: u32,
        rhs: u32,
    },
    AnyRise {
        lhs: u32,
        rhs: u32,
    },
    /// The owner's activation definition, regenerated at this reserved
    /// ordinal by the staged replay consuming the reservation.
    QuotientOwner(u32),
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum RelationEntryWire {
    Relation {
        expression: u32,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
    /// The owner's relation, regenerated at exactly this stream position.
    QuotientOwner { owner: u32 },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ConditionEntryWire {
    pub(super) node: Option<ConditionNodeWire>,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum RootEntryWire {
    Root {
        relation: u32,
        activation: u32,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
    /// The owner's root, regenerated at exactly this stream position.
    QuotientOwner { owner: u32 },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct StructuredRootEntryWire {
    pub(super) domain: u32,
    pub(super) expression: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct TimeEventEntryWire {
    pub(super) instant: Option<ClockRationalWire>,
    pub(super) deadline: Option<u32>,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum EventActionKindWire {
    Assert { message: u32, level: Option<u32> },
    Terminate { message: u32 },
    Reinitialize { state: u32, value: u32 },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct EventActionEntryWire {
    pub(super) trigger: u32,
    pub(super) guard: u32,
    pub(super) kind: EventActionKindWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum ClockKindWire {
    Periodic(PeriodicClockScheduleWire),
    Triggered(u32),
}

#[derive(Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
pub(super) struct ClockRationalWire {
    pub(super) num: i128,
    pub(super) den: i128,
}

impl ClockRationalWire {
    pub(super) fn checked(
        self,
        at: DaeProvenance,
    ) -> Result<rumoca_core::ClockRational, DaeConstructionError> {
        rumoca_core::ClockRational::new(self.num, self.den).map_err(|source| {
            DaeConstructionError::InvalidClockLattice {
                source,
                span: at.span(),
            }
        })
    }
}

#[derive(Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
pub(super) struct ClockLatticeWire {
    pub(super) period: ClockRationalWire,
    pub(super) phase: ClockRationalWire,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub(super) enum ClockPhaseAnchorWire {
    Absolute,
    SimulationStart,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
pub(super) struct PeriodicClockScheduleWire {
    pub(super) lattice: ClockLatticeWire,
    pub(super) anchor: ClockPhaseAnchorWire,
}

impl PeriodicClockScheduleWire {
    pub(super) fn checked(
        self,
        at: DaeProvenance,
    ) -> Result<rumoca_core::PeriodicClockSchedule, DaeConstructionError> {
        let lattice = self.lattice.checked(at)?;
        let result = match self.anchor {
            ClockPhaseAnchorWire::Absolute => rumoca_core::PeriodicClockSchedule::absolute(lattice),
            ClockPhaseAnchorWire::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(lattice)
            }
        };
        result.map_err(|source| DaeConstructionError::InvalidClockLattice {
            source,
            span: at.span(),
        })
    }
}

impl ClockLatticeWire {
    pub(super) fn checked(
        self,
        at: DaeProvenance,
    ) -> Result<rumoca_core::ClockLattice, DaeConstructionError> {
        rumoca_core::ClockLattice::new(self.period.checked(at)?, self.phase.checked(at)?).map_err(
            |source| DaeConstructionError::InvalidClockLattice {
                source,
                span: at.span(),
            },
        )
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ClockEntryWire {
    pub(super) kind: ClockKindWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ClockOwnershipEntryWire {
    pub(super) variable: u32,
    pub(super) clock: u32,
    pub(super) sampled: bool,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct PreviousEntryWire {
    pub(super) variable: u32,
    pub(super) clock: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct TerminalEntryWire {
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct PositiveParameterWire {
    pub(super) expression: u32,
    pub(super) value: f64,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum DelayKindWire {
    ParameterDelay {
        delay_time: PositiveParameterWire,
    },
    BoundedDelay {
        delay_time: u32,
        delay_max: PositiveParameterWire,
    },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct DelayEntryWire {
    pub(super) source: u32,
    pub(super) kind: DelayKindWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    pub(super) provenance: DaeProvenance,
}
