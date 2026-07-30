use std::marker::PhantomData;

use crate::clocks::ClockedVariableRole;
use crate::model::{Storage, check_provenance, checked_u32, unknown};
use crate::{
    ClockId, DaeConstructionError, DaeProvenance, DelayId, DiscreteRealId, DiscreteValueId, ExprId,
    ExpressionVariability, PreviousId, ScalarType, TerminalId, ValueType, VariableId,
};

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct PreviousEntry {
    pub(crate) variable: u32,
    #[serde(skip_serializing)]
    pub(crate) role: ClockedVariableRole,
    pub(crate) clock: u32,
    #[serde(skip_serializing)]
    pub(crate) value_type: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct TerminalEntry {
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct PositiveParameterEntry {
    pub(crate) expression: u32,
    pub(crate) value: f64,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DelayEntry {
    pub(crate) source: u32,
    pub(crate) kind: DelayKind,
    #[serde(skip_serializing)]
    pub(crate) value_type: u32,
    #[serde(skip_serializing)]
    pub(crate) variability: ExpressionVariability,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum DelayKind {
    ParameterDelay {
        delay_time: PositiveParameterEntry,
    },
    BoundedDelay {
        delay_time: u32,
        delay_max: PositiveParameterEntry,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct PreviousView<'dae> {
    pub(crate) variable: VariableId<'dae>,
    pub(crate) clock: ClockId<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> PreviousView<'dae> {
    pub const fn variable(self) -> VariableId<'dae> {
        self.variable
    }

    pub const fn clock(self) -> ClockId<'dae> {
        self.clock
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TerminalView {
    pub(crate) provenance: DaeProvenance,
}

impl TerminalView {
    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PositiveParameterView<'dae> {
    pub(crate) expression: ExprId<'dae>,
    pub(crate) value: f64,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> PositiveParameterView<'dae> {
    pub const fn expression(self) -> ExprId<'dae> {
        self.expression
    }

    pub const fn value(self) -> f64 {
        self.value
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DelayOperation<'dae> {
    ParameterDelay {
        delay_time: PositiveParameterView<'dae>,
    },
    BoundedDelay {
        delay_time: ExprId<'dae>,
        delay_max: PositiveParameterView<'dae>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct DelayView<'dae> {
    pub(crate) source: ExprId<'dae>,
    pub(crate) operation: DelayOperation<'dae>,
    pub(crate) value_type: &'dae ValueType,
    pub(crate) variability: ExpressionVariability,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> DelayView<'dae> {
    pub const fn source(self) -> ExprId<'dae> {
        self.source
    }

    pub const fn operation(self) -> DelayOperation<'dae> {
        self.operation
    }

    pub const fn value_type(self) -> &'dae ValueType {
        self.value_type
    }

    pub const fn variability(self) -> ExpressionVariability {
        self.variability
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

pub struct PositiveParameter<'dae> {
    pub(crate) entry: PositiveParameterEntry,
    marker: PhantomData<&'dae mut &'dae ()>,
}

#[derive(Debug, Clone, Copy)]
pub struct DelayCoordinate<'dae> {
    id: DelayId<'dae>,
    expression: ExprId<'dae>,
}

impl<'dae> DelayCoordinate<'dae> {
    pub(crate) const fn new(id: DelayId<'dae>, expression: ExprId<'dae>) -> Self {
        Self { id, expression }
    }

    pub const fn id(self) -> DelayId<'dae> {
        self.id
    }

    pub const fn expression(self) -> ExprId<'dae> {
        self.expression
    }
}

pub struct Temporal<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Temporal<'_, 'dae> {
    pub fn positive_parameter(
        &self,
        expression: ExprId<'dae>,
        finite_value: f64,
        provenance: DaeProvenance,
    ) -> Result<PositiveParameter<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .expect_closed_expression(expression, provenance)?;
        let ty = self.storage.expr_type(expression, provenance)?;
        let variability = self.storage.expr_variability(expression, provenance)?;
        if !ty.is_scalar()
            || ty.scalar_type() != ScalarType::Real
            || variability > ExpressionVariability::Parameter
            || !finite_value.is_finite()
            || finite_value <= 0.0
        {
            return Err(DaeConstructionError::InvalidPositiveParameter {
                span: provenance.span(),
            });
        }
        Ok(PositiveParameter {
            entry: PositiveParameterEntry {
                expression: expression.index(),
                value: finite_value,
                provenance,
            },
            marker: PhantomData,
        })
    }

    pub fn previous_discrete_real(
        &mut self,
        clock: ClockId<'dae>,
        variable: DiscreteRealId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<PreviousId<'dae>, DaeConstructionError> {
        self.previous(
            clock,
            variable.index(),
            ClockedVariableRole::DiscreteReal,
            provenance,
        )
    }

    pub fn previous_discrete_value(
        &mut self,
        clock: ClockId<'dae>,
        variable: DiscreteValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<PreviousId<'dae>, DaeConstructionError> {
        self.previous(
            clock,
            variable.index(),
            ClockedVariableRole::DiscreteValue,
            provenance,
        )
    }

    pub fn terminal(
        &mut self,
        provenance: DaeProvenance,
    ) -> Result<TerminalId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        if !self.storage.terminals.is_empty() {
            return Ok(TerminalId::from_raw(0));
        }
        self.storage.terminals.push(TerminalEntry { provenance });
        Ok(TerminalId::from_raw(0))
    }

    fn previous(
        &mut self,
        clock: ClockId<'dae>,
        variable: u32,
        role: ClockedVariableRole,
        provenance: DaeProvenance,
    ) -> Result<PreviousId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .clocks
            .get(clock.index() as usize)
            .ok_or_else(|| unknown("clock", clock.index(), provenance))?;
        let ownership_exists = self.storage.clock_ownerships.iter().any(|ownership| {
            ownership.variable == variable
                && ownership.clock == clock.index()
                && ownership.role == role
        });
        if !ownership_exists {
            return Err(DaeConstructionError::MissingClockOwnership {
                variable,
                clock: clock.index(),
                span: provenance.span(),
            });
        }
        if let Some(index) = self.storage.previous_values.iter().position(|entry| {
            entry.variable == variable && entry.clock == clock.index() && entry.role == role
        }) {
            return Ok(PreviousId::from_raw(index as u32));
        }
        let value_type = self.storage.variable(variable, provenance)?.value_type;
        let raw = checked_u32(
            self.storage.previous_values.len(),
            "previous-value arena",
            provenance,
        )?;
        self.storage.previous_values.push(PreviousEntry {
            variable,
            role,
            clock: clock.index(),
            value_type,
            provenance,
        });
        Ok(PreviousId::from_raw(raw))
    }
}
